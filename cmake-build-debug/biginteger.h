#include <iostream>
#include <vector>
#include <string>


class BigInteger;
std::istream& operator >> (std::istream& cin, BigInteger& input);
std::ostream& operator << (std::ostream& cout, BigInteger& output);



class BigInteger {
    friend std::ostream& operator << (std::ostream& cout, BigInteger& output);

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

    void operator+= (const BigInteger& other) {
        if ((negative() && other.negative()) || (!negative() && !other.negative())) {
            plus_if_same_sign(other);
        } else {
            minus_if_same_sign(other);
        }
    }

    void operator-= (const BigInteger& other) {
        if ((!negative() && !other.negative()) || (negative() && other.negative())) {
            minus_if_same_sign(other);
        } else {
            plus_if_same_sign(other);
        }
    }

    void operator*= (const BigInteger& other) {   //TODO (nˆ2 - unoptimized)
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
    }

    /*BigInteger div_first(BigInteger& other) {
        BigInteger return_val = BigInteger();
        return_val.body_.clear();
        for (int i = 0; i < other.size() / 2; ++i)
            return_val.body_.push_back(other.body_[i]);
        return return_val;
    }

    BigInteger div_second(BigInteger& other) {
        BigInteger return_val = BigInteger();
        return_val.body_.clear();
        for (int i = other.size() / 2; i < other.size(); ++i)
            return_val.body_.push_back(other.body_[i]);
        return return_val;
    }

    BigInteger add_right(BigInteger& other) {
        BigInteger return_val = BigInteger();
        return_val.body_.clear();
        for (int i = 0; i < size(); ++i) {
            return_val.body_.push_back(body_[i]);
        }
        for (int i = 0; i < other.size(); ++i) {
            return_val.body_.push_back(other.body_[i]);
        }
        return return_val;
    }

    BigInteger mult(BigInteger a, BigInteger other) {
        BigInteger ans = BigInteger();
        if (a.size() == 0)
            return ans;
        if (a.size() == 1) {
            a *= other;
            return a;
        }
        if (other.size() == 1) {
            a *= other;
            return a;
        }
        BigInteger res = BigInteger();
        res.body_.clear();
        res.body_.resize(a.size() + other.size() + 2);
        BigInteger left_this = div_first(a);
        BigInteger right_this = div_second(a);
        BigInteger left_other = div_first(other);
        BigInteger right_other = div_second(other);
        std::cout << left_this << ' ' << right_this << ' ' << left_other << ' ' << right_other << std::endl;

        BigInteger a1 = mult(left_this, left_other);
        std::cout << "fi " << a1 << std::endl;
        for (int i = 0; i < a1.size(); ++i) {
            res.body_[i] += a1.body_[i];
            if (res.body_[i] > 9) {
                res.body_[i] -= 10;
                res.body_[i + 1]++;
            }
        }
        a1 = mult(left_this, right_other);
        std::cout << "se " << a1 << std::endl;
        for (int i = left_other.size(); i < left_other.size() + a1.size(); ++i) {
            res.body_[i] += a1.body_[i - left_other.size()];
            if (res.body_[i] > 9) {
                res.body_[i] -= 10;
                res.body_[i + 1]++;
            }
        }
        a1 = mult(right_this, left_other);
        std::cout << "th " << a1 << std::endl;
        for (int i = left_this.size(); i < left_this.size() + a1.size(); ++i) {
            res.body_[i] += a1.body_[i - left_this.size()];
            if (res.body_[i] > 9) {
                res.body_[i] -= 10;
                res.body_[i + 1]++;
            }
        }
        a1 = mult(right_this, right_other);
        std::cout << "fo " << a1 << std::endl;
        for (int i = left_this.size() + left_other.size(); i < left_this.size() + left_other.size() + a1.size(); ++i) {
            res.body_[i] += a1.body_[i - (left_this.size() + left_other.size())];
            if (res.body_[i] > 9) {
                res.body_[i] -= 10;
                res.body_[i + 1]++;
            }
        }
        for (int i = 0; i < res.size() - 1; ++i) {
            while (res.body_[i] > 9) {
                res.body_[i] -= 10;
                res.body_[i + 1]++;
            }
        }
        while (res.size() > 1 && res.body_[res.size() - 1] == 0)
            res.body_.pop_back();

        std::cout << res << std::endl;
        res.negative_ = (a.negative_ != other.negative_);
        return res;
    }*/

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

    void operator/= (const BigInteger& other) {
        BigInteger a = BigInteger();
        BigInteger new_body = BigInteger();
        division(other, a, new_body);
        *this = new_body;
        normalize();
    }

    void operator%= (const BigInteger& other) {
        BigInteger a = BigInteger();
        BigInteger new_body = BigInteger();
        division(other, a, new_body);
        *this = a;
        normalize();
    }
};

const int BigInteger::NUMBER_COUNT = 1;
const int BigInteger::BASE = 10;

std::istream& operator >> (std::istream& cin, BigInteger& input) {
    std::string new_input;
    std::cin >> new_input;
    input = BigInteger(new_input);
    return cin;
}

std::ostream& operator << (std::ostream& cout, BigInteger& output) {
    if (output.negative())
        cout << '-';
    int max_size = static_cast<int>(output.body_.size()) - 1;
    for (int i = max_size; i >= 0; --i) {
        if (i != max_size) {
            int base_number = BigInteger::BASE;
            while (output.body_[i] < base_number / 10) {
                cout << 0;
                base_number /= 10;
            }
        }
        std::cout << output.body_[i];
    }
    return cout;
}

