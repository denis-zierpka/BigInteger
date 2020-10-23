#include <iostream>
#include <vector>
#include <string>


class BigInteger;
std::istream& operator >> (std::istream& cin, BigInteger& input);
std::ostream& operator << (std::ostream& cout, BigInteger& output);



class BigInteger {
    friend std::ostream& operator << (std::ostream& cout, const BigInteger& output);

public:
    static const int NUMBER_COUNT;
    static const int BASE;

private:
    std::vector<int> body_;
    bool negative_;

public:

    BigInteger(): body_({0}), negative_(false) {}

    BigInteger(const BigInteger& other): body_(other.body_), negative_(other.negative_) {}

    explicit BigInteger(const std::string& value): BigInteger() {
        body_.clear();
        negative_ = false;
        int is_negative = 0;
        if (value[0] == '-') {
            negative_ = true;
            is_negative = 1;
        }
        for (int i = static_cast<int>(value.size()) - 1; i >= is_negative; --i) {
            int max_number_of_symbols = std::min(NUMBER_COUNT, i - is_negative + 1);
            int new_number = 0;
            int base_change = 1;
            for (int j = i; j > i - max_number_of_symbols; --j) {
                new_number += (value[j] - '0') * base_change;
                base_change *= 10;
            }
            i -= max_number_of_symbols - 1;
            body_.push_back(new_number);
        }
    }

    BigInteger(const int element): BigInteger(std::to_string(element)) {}

    std::string toString() {
        std::string new_object;
        if (negative())
            new_object += '-';
        int max_size = static_cast<int>(body_.size()) - 1;
        for (int i = max_size; i >= 0; --i) {
            if (i != max_size) {
                std::string base_number = std::to_string(body_[i]);
                for (int j = 0; j < BigInteger::NUMBER_COUNT - static_cast<int>(base_number.size()); ++j)
                    new_object += '0';
            }
            new_object += std::to_string(body_[i]);
        }
        return new_object;
    }

    bool negative() const {
        return negative_;
    }

    void swap(BigInteger& other) {
        std::swap(body_, other.body_);
        std::swap(negative_, other.negative_);
    }

    int size() const {
        return static_cast<int>(body_.size());
    }

    BigInteger operator-() const {
        BigInteger new_object = *this;
        new_object.negative_ = !new_object.negative_;
        new_object.normalize();
        return new_object;
    }

    void normalize() {
        while (size() > 1 && body_[size() - 1] == 0)
            body_.pop_back();
        if (size() == 0) {
            body_.push_back(0);
            negative_ = false;
        } else if (size() == 1 && body_[0] == 0) {
            negative_ = false;
        }
    }

    bool more_if_equal_sign(const BigInteger& other) const {
        if (size() > other.size()) {
            return true;
        } else if (size() < other.size()) {
            return false;
        } else {
            for (int i = size() - 1; i >= 0; --i) {
                if (body_[i] < other.body_[i])
                    return false;
                if (body_[i] > other.body_[i])
                    return true;
            }
            return false;
        }
    }

    bool operator> (const BigInteger& other) const {
        if (!negative() && other.negative()) {
            return true;
        } else if (negative() && !other.negative()) {
            return false;
        } else if (!negative() && !other.negative()) {
            return more_if_equal_sign(other);
        } else {
            return other.more_if_equal_sign(*this);
        }
    }

    explicit operator bool() const {
        return (*this != 0);
    }

    bool operator< (const BigInteger& other) const {
        return (other > *this);
    }

    bool operator== (const BigInteger& other) const {
        return (size() == other.size() && !(*this > other) && !(other > *this));
    }

    bool operator>= (const BigInteger& other) const {
        return (*this > other || *this == other);
    }

    bool operator<= (const BigInteger& other) const {
        return (*this < other || *this == other);
    }

    bool operator!= (const BigInteger& other) const {
        return !(*this == other);
    }

    void plus_if_same_sign(const BigInteger& other) {
        while (other.size() > size())
            body_.push_back(0);
        for (int i = 0; i < std::max(size(), other.size()); ++i) {
            if (i < other.size())
                body_[i] += other.body_[i];
            if (body_[i] >= BASE) {
                body_[i] -= BASE;
                if (i + 1 < size()) {
                    body_[i + 1]++;
                } else {
                    body_.push_back(1);
                }
            }
        }
        while (size() > 1 && body_[size() - 1] == 0)
            body_.pop_back();

        normalize();
    }

    void minus_if_same_sign(const BigInteger& other) {
        bool second_more = false;
        if (!more_if_equal_sign(other)) {
            second_more = true;
        }
        while (other.size() > size())
            body_.push_back(0);
        for (int i = 0; i < std::max(size(), other.size()); ++i) {
            if (i < other.size()) {
                if (second_more) {
                    body_[i] += other.body_[i] - 2 * body_[i];
                } else {
                    body_[i] -= other.body_[i];
                }
            }
        }
        for (int i = 0; i < std::max(size(), other.size()); ++i) {
            if (body_[i] < 0) {
                body_[i] += BASE;
                body_[i + 1]--;
            }
        }

        while (size() > 1 && body_[size() - 1] == 0)
            body_.pop_back();
        if (second_more)
            negative_ = !negative_;

        normalize();
    }

    BigInteger& operator+= (const BigInteger& other) {
        if ((negative() && other.negative()) || (!negative() && !other.negative())) {
            plus_if_same_sign(other);
        } else {
            minus_if_same_sign(other);
        }
        return *this;
    }

    BigInteger& operator++() {
        *this += 1;
        return *this;
    }

    BigInteger operator++(int) {
        BigInteger new_object = *this;
        *this += 1;
        return new_object;
    }

    BigInteger& operator-= (const BigInteger& other) {
        if ((!negative() && !other.negative()) || (negative() && other.negative())) {
            minus_if_same_sign(other);
        } else {
            plus_if_same_sign(other);
        }
        return *this;
    }

    BigInteger& operator*= (const BigInteger& other) {   //TODO (nˆ2 - unoptimized)
        int new_size = size() + other.size() + 2;
        BigInteger new_body = BigInteger();
        new_body.body_.resize(new_size);
        for (int i = 0; i < size(); ++i) {
            for (int j = 0; j < other.size(); ++j) {
                new_body.body_[i + j] += body_[i] * other.body_[j];
                new_body.body_[i + j + 1] += new_body.body_[i + j] / BASE;
                new_body.body_[i + j] %= BASE;
            }
        }
        for (int i = 0; i < size(); ++i) {
            if (new_body.body_[i] >= BASE) {
                new_body.body_[i + 1] += new_body.body_[i] / BASE;
                new_body.body_[i] %= BASE;
            }
        }
        while (new_body.size() > 1 && new_body.body_[new_body.size() - 1] == 0) {
            new_body.body_.pop_back();
        }
        new_body.negative_ = (negative_ != other.negative_);
        swap(new_body);
        return *this;
    }

    void division(const BigInteger& other, BigInteger& a, BigInteger& new_body) {
        if (other == 1 || other == -1) {
            negative_ = (negative_ != other.negative_);
            return;
        }
        for (int i = size() - 1; i >= 0;) {
            while (other.more_if_equal_sign(a)) {
                a *= BASE;
                a += body_[i];
                i--;
                if (i == -1)
                    break;
            }
            int q = 0;
            while (a.more_if_equal_sign(other) || a == other) {
                if (!other.negative())
                    a -= other;
                else
                    a += other;
                ++q;
            }
            new_body *= BASE;
            new_body += q;
        }
        new_body.negative_ = (negative_ != other.negative_);
        a.negative_ = negative_;
    }

    BigInteger& operator/= (const BigInteger& other) {
        BigInteger a = BigInteger();
        BigInteger new_body = BigInteger();
        division(other, a, new_body);
        *this = new_body;
        normalize();
        return *this;
    }

    BigInteger& operator%= (const BigInteger& other) {
        BigInteger a = BigInteger();
        BigInteger new_body = BigInteger();
        division(other, a, new_body);
        *this = a;
        normalize();
        return *this;
    }
};

const int BigInteger::NUMBER_COUNT = 4;
const int BigInteger::BASE = 10000;

BigInteger operator+ (const BigInteger& object, const BigInteger& other) {
    BigInteger new_object = object;
    new_object += other;
    return new_object;
}

BigInteger operator- (const BigInteger& object, const BigInteger& other) {
    BigInteger new_object = object;
    new_object -= other;
    return new_object;
}

BigInteger operator* (const BigInteger& object, const BigInteger& other) {
    BigInteger new_object = object;
    new_object *= other;
    return new_object;
}

BigInteger operator/ (const BigInteger& object, const BigInteger& other) {
    BigInteger new_object = object;
    new_object /= other;
    return new_object;
}

BigInteger operator% (const BigInteger& object, const BigInteger& other) {
    BigInteger new_object = object;
    new_object %= other;
    return new_object;
}

std::istream& operator >> (std::istream& cin, BigInteger& input) {
    std::string new_input;
    std::cin >> new_input;
    input = BigInteger(new_input);
    return cin;
}

std::ostream& operator << (std::ostream& cout, const BigInteger& output) {
    if (output.negative())
        cout << '-';
    int max_size = static_cast<int>(output.body_.size()) - 1;
    for (int i = max_size; i >= 0; --i) {
        if (i != max_size) {
            std::string base_number = std::to_string(output.body_[i]);
            for (int j = 0; j < BigInteger::NUMBER_COUNT - static_cast<int>(base_number.size()); ++j)
                cout << 0;
        }
        std::cout << output.body_[i];
    }
    return cout;
}
